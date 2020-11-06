package hyperfiddle;
import hyperfiddle.Meta;
// import haxe.macro.Expr;
// import haxe.macro.Context;
// import haxe.macro.Compiler;
using Lambda;
using hyperfiddle.Meta.X;

@:publicFields class Origin {                       // public API singleton
  static var main : Flow;
  static function get() return if(main != null) main else main = new Flow();
  static function all(f) get().all(f);

  static var onError : Error -> Void = (error) -> trace(error);

  static function input<A>(?f) {                                    // f is the lifecycle fn
    return new Input<A>(get(), new Push(From({
      var end = null;
      { on: () -> if(f != null) end = f(),                          // f can return end continuation
        off: () -> if(end != null) {end(); end = null;} } })));     // lifecycle state
  }

  static function on<A>(v : View<A>, f : A -> Void) {               // terminal node
    // f is an effect callback
    var out =  new Output(get(), new Push([v.node], Into(f)));
    out.init();   // propogate that someone is listening
    return out;
  }

  static function apply(ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push([for(n in ns) n.node], ApplyN(f))); // set the inbound edges
  }

  static function applyAsync(ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push([for(n in ns) n.node], ApplyAsync(f))); // set the inbound edges
  }
}

@:publicFields class View<A> {
  var F : Flow;
  var node : Push<A>;
  function new(f, n) {F = f; node = n;}
}

@:publicFields class Input<A> extends View<A> {
  function put(a : A) {F.put(node, Val(a));}
  function end() {F.put(node, End);}
}

@:publicFields class Output<A> extends View<A> {
  function init() {F.update(node);}                 // Indicate someone is listening
  function off() {F.put(node, End);}                // Indicate stopped listening
}

enum NodeDef<T> {       // GT the NodeDef values essentially define a live AST of what should be done
  From<A>(source : {on : () -> Void, off : () -> Void}) : NodeDef<A>;
  Into<A>(f : A -> Void);                                   // terminal node
  ApplyN<A>(f : Array<Dynamic> -> A) : NodeDef<A>;
  ApplyAsync<A>(f : Array<Dynamic> -> (A -> Void) -> Void) : NodeDef<A>;
}

enum Action<A> {
  Val(a : A);
  Error(e : A);
  End;
}

enum Maybe<A> {
  Just(a : A);
  Nothing;
}

typedef Frame = Int;
typedef Rank = Int;

@:nullSafety(Loose)
@:publicFields class Flow {                                 // singleton
  static var count = 0;
  static function id() {return ++count;}

  var lock : Bool = false;
  var frame : Frame = 0;                                    // ?
  var queue : Array<Array<Push<Dynamic>>> = [];             // Many pushes, grouped by rank

  function new() {}

  function put<A>(node : Push<A>, a : Action<A>) {          // end goes backwards, val goes forwards
    node.put(a);                                            // plan it
    var node = node.unlink();                               // remove from old order (do it now)
    if(node != null) add(node);                             // do it now
    run();                                                  // now
  }

  function all(f : Void -> Void) {                          // batch put in one frame
    lock = true;
    var e : Null<Error> = null;
    try f() catch(x : Error) e = x;
    lock = false;
    run();
    onError(e);
  }

  function add(node : Push<Dynamic>) {      // queue
    if(node.queued) return;
    while(queue.length <= node.rank) queue.push([]);
    queue[node.rank].push(node);
    node.queued = true;
  }

  function run() {                          // Run queue until empty
    if(lock) return;
    lock = true;

    frame++;

    var e : Null<Error> = null;
    try { // Flow owns the queue. Flow chooses what order the nodes will run in and runs them in that order.
      var rank = 0;                         // for ordering dependencies

      while(rank < queue.length) {
        for(node in queue[rank])
          node.run(this);                   // compute the node and plan what happens next

        for(node in queue[rank]) {          // execute the plan we just planned
          var node = node.unlink();         // already computed this one
          while(node != null) {             // the plan is a linked list
            if(node.rank == rank)           //
              node.run(this);               // compute the node and propogate forward
            else if(node.rank > rank)       // not quite yet
              add(node);                    // queue it for when this rank runs
            node = node.unlink();           // done this node, continue next
          }
        }

        for(node in queue[rank])
          clear(node);                      // mark join nodes as not-ok, but why?

        queue[rank].resize(0);              // empty this layer of queue

        rank++;
      }
    }
    catch(x : Error) e = x;
    lock = false;
    onError(e);
  }

  function into<A>(n : Push<A>, f : Null<A> -> Void, val : Null<A>) {
    f(val);
  }

  function onError(?e) {
    if(e != null)
      Origin.onError(e);
  }

  function clear(n : Push<Dynamic>) {
    if(!n.queued) return;                           // ?
    n.queued = false;

    if(!n.to.opt().exists(x -> x.joins()))          // join nodes only
      n.val = Nothing;                                // mark not ok, but why?

    for(x in n.on.opt()) clear(x);                  // propogate backwards
  }

  function update(a : Push<Dynamic>) {              // rename "init" ?
    if(!a.active()) return;                         // it could be an Into i guess?
    a.rank = 0;
    for(x in a.on.opt()) {                          // inbound edges
      attach(x, a);                                 // setup reverse links
      if(x.rank > a.rank) a.rank = x.rank;
    }
    if(a.joins()) a.rank++;                         // run after all dependencies
  }

  function attach(a : Push<Dynamic>, b : Push<Dynamic>) { // set reverse links
    // Reverse links aren't set until someone is listening ... which is now
    if(a.to == null) {                              // first listener
      a.to = [b];                                   // set the first backlink
      switch(a.def) {
        case From(source):                          // if it is the origin
          if(source.on != null) source.on();        // fire lifecycle at the origin
        default:
      }
    }
    else if(!a.to.has(b))                           // its a set
      a.to.push(b);
    update(a);
  }

  function detach(a : Push<Dynamic>, b : Push<Dynamic>) {
    if(a.to.ok() && a.to.remove(b)) {
      if(a.to.nil())                                // no more left
        switch(a.def) {
          case From(source):
            if(source.off != null) source.off();    // lifecycle
          default:                                  // traverse backwards until we find the source
            for (x in a.on.opt()) detach(x, a); // ...
        }
    }
  }
}

@:nullSafety(Loose)
@:publicFields class Push<A> {
  var def : NodeDef<A>;
  var on : Null<Array<Push<Dynamic>>>;
  var to : Null<Array<Push<Dynamic>>>;

  var id : Int = Flow.id();
  var rank : Rank = 0;
  var frame : Frame = 0;              // due to join, nodes can be at different frames?
  var queued : Bool = false;

  var val : Maybe<A> = Nothing;
  var error : Null<Dynamic>;
  var ended : Bool = false;

  var next : Null<Push<Dynamic>>;
  var prev : Null<Push<Dynamic>>;

  function new(?ns : Array<Push<Dynamic>>, d) {
    def = d;
    if(ns != null) on = ns.copy();
  }

  function toString() return 'Push($id, $rank, $def)';

  function ok()     return val != Nothing && !ended;
  function active() return !ended && (to != null || def.match(Into(_)));
  function joins()  return on.ok() && on.length > 1;

  function extract(a : Maybe<A>) : Null<A>{
    return switch(a){
      case Just(a) : a;
      case Nothing : null;
    }
  }

  function run(F : Flow) {                          // run this layer of the applicative functor and push effect forward
    if(frame == F.frame) return;                    // already ran this node?
    frame = F.frame;                                // Mark ran

    if(!active()) return;                           // Skip the work, nobody is listening

    try switch(def) {
      case From(_):  {}
        case Into(_), ApplyN(_), ApplyAsync(_):
        if(on.ok() && on.foreach(n -> n.ok()))      // all dependencies are already propagated, check for ends
          switch(def) {
            case Into(f):   F.into(this, cast f, cast on[0].val);
            case ApplyN(f): put(Val((cast f)([for(n in on) n.val])));
            case ApplyAsync(f): (cast f)([for(n in on) n.val], (success, v) -> {
                                  if (success){
                                    F.put(this, Val(v));
                                  } else {
                                    throw v;
                                  }
                                });
            default:        {}
          }
        else if(on.opt().exists(n -> n.ended))      // if my inbound edges are ended, end me
          put(End);
        for(x in on.opt()) if(x.error != null)
          throw x.error;
    }
    catch(v : Dynamic){
      put(Error(v));
    }
  }

  function put(a : Action<A>) {
    switch(a) {
      case Val(v):   val = Just(v);                 // memory
      case Error(e): error = e;
      case End:      ended = true;
    }
    push();             // Plan this node's outbound edges (they will be run at Flow.put level)
  }

  function push() {                                 // Plan out the order of the computation
    if(to == null) return;
    var n : Push<Dynamic> = this;                   // cast away A
    for(x in to) {                                  // outgoing edges run after this runs
      if(x.queued) continue;                        // ?
      n.link(x);                                    // link each node as part of propagation ?
      n = x;                                        // this happens in a deterministic order
    }
  }

  function link(n : Push<Dynamic>) {                // Order this node next (splice here)
    if(n == next) return;                           // already in the right place
    n.unlink();                                     // remove it from its current order so we can put it sooner
    n.next = next;                                  //   e.g. a diamond
    n.prev = this;
    next = n;
  }

  function unlink() {
    if(prev != null) prev.next = next;
    if(next != null) next.prev = prev;
    var n = next;
    prev = null; next = null;
    return n;
  }
}
