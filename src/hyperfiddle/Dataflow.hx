package hyperfiddle;
import hyperfiddle.Meta;
using Lambda;
using hyperfiddle.Meta.X;

@:publicFields class Origin {                       // public API singleton
  static var main : Flow;
  static function get() return if(main != null) main else main = new Flow();
  static function all(f) get().all(f);

  static var onError : Error -> Void = (error) -> trace(error);

  static function input<A>(?name, ?f) {
    return new Input<A>(get(), new Push(name, From({
      var end = null;
      { on: () -> if(f != null) end = f(),
        off: () -> if(end != null) {end(); end = null;} } })));
  }

  static function on<A>(?name, v : View<A>, f : A -> Void) {               // terminal node
    // f is an effect callback
    var out =  new Output(get(), new Push(name, [v.node], Into(f)));
    out.init();   // propogate that someone is listening
    return out;
  }

  static function apply(?name, ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push(name, [for(n in ns) n.node], ApplyN(f))); // set the inbound edges
  }

  static function applyAsync(?name, ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push(name, [for(n in ns) n.node], ApplyAsync(f))); // set the inbound edges
  }

  static function pure<A>(?name, a: A) {
    return new View(get(), new Push(name, Const(a)));
  }

  static function bind<A>(?name, n: View<Dynamic>, f: Dynamic -> View<A>) {
    return new View(get(), new Push(name, [n.node], Bind(f)));
  }
}

@:publicFields class View<A> {
  var F : Flow;
  var node : Push<A>;
  function new(f, n) {F = f; node = n;}
}

@:publicFields class Input<A> extends View<A> {
  function put(a : A) {F.resume(node, Val(a));}
  function end() {F.resume(node, End);}
}

@:publicFields class Output<A> extends View<A> {
  function init() {F.activate(node);}                 // Indicate someone is listening
  function off() {F.resume(node, End);}                // Indicate stopped listening
}

enum NodeDef<T> {       // GT the NodeDef values essentially define a live AST of what should be done
  Const<A>(a: A) : NodeDef<A>;
  From<A>(source : {on : () -> Void, off : () -> Void}) : NodeDef<A>;
  Into<A>(f : A -> Void);                                   // terminal node
  ApplyN<A>(f : Array<Dynamic> -> A) : NodeDef<A>;
  ApplyAsync<A>(f : Array<Dynamic> -> (A -> Void) -> (A -> Void) -> Void) : NodeDef<A>;
  Bind<A>(f : Dynamic -> View<A>) : NodeDef<A>;
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

@:nullSafety(Loose)
@:publicFields class Flow {                                 // singleton
  static var count = 0;
  static function id() {return ++count;}

  var lock : Bool = false;
  var frame : Int = 0;                                    // ?
  var queue : Array<Array<Push<Dynamic>>> = [];             // Many pushes, grouped by rank

  function new() {}

  function resume<A>(node : Push<A>, a : Action<A>) {
    node.resume(a);
    var node = node.shift();
    if(node != null) add(node);
    run();
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
    if(lock) { trace("Flow.run lock=true"); return; }
    lock = true;

    frame++;

    var e : Null<Error> = null;
    try { // Flow owns the queue. Flow chooses what order the nodes will run in and runs them in that order.
      var rank = 0;                         // for ordering dependencies

      while(rank < queue.length) {
        for(node in queue[rank])
          // can node.run return its plan?
          node.run(this);                   // compute the node and plan what happens next

        // loop through the new plan, either queing or running now
        for(node in queue[rank]) {          // execute the plan we just planned
          var node = node.shift();         // already computed this one
          while(node != null) {             // the plan is a linked list
            if(node.rank == rank)           //
              node.run(this);               // compute the node and propogate forward
            else if(node.rank > rank)       // not quite yet
              add(node);                    // queue it for when this rank runs
            node = node.shift();           // done this node, continue next
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

  function clear(b : Push<Dynamic>) {
    if(!b.queued) return;                           // ?
    b.queued = false;

//    if(!b.to.opt().exists(c -> c.joins()))
//      b.val = Nothing;                                // mark not ok, but why?

    for(a in b.on.opt()) clear(a);                  // propogate backwards
  }

  function activate(b : Push<Dynamic>) {
    //trace("activate ", b.def);
    if(!b.active()) return;
    b.rank = 0;
    for(a in b.on.opt()) {
      attach(a, b);
      if(a.rank > b.rank) b.rank = a.rank; // greatest dependency
    }
    //b.rank = [for(a in b.on.opt()) a.rank].fold(Math.max, 0);
    if(b.joins()) b.rank++;
  }

  function attach(a : Push<Dynamic>, b : Push<Dynamic>) {
    //trace("attach ", a.def, b.def);
    if(a.to == null) {
      a.to = [b];
      switch(a.def) {
        case From(source):
          if(source.on != null) source.on();
          //put(a, a.val) // forceUpdate
        case Const(v):
          resume(a, Val(v));
        default:
      }
    }
    else if(!a.to.has(b))
      a.to.push(b);
    activate(a);
  }

  function detach(b : Push<Dynamic>, c : Push<Dynamic>) {
    //trace("detach ", b.def, c.def);
    if(b.to.ok() && b.to.remove(c)) {
      if(b.to.nil())
        switch(b.def) {
          case From(source):
            if(source.off != null) source.off();
          default:
            for (a in b.on.opt()) detach(a, b); // traverse backwards until we find the source
        }
    }
  }
}

@:nullSafety(Loose)
@:publicFields class Push<A> {
  var def : NodeDef<A>;
  var on : Null<Array<Push<Dynamic>>>; // upstream lookup (value dependencies)
  var to : Null<Array<Push<Dynamic>>>; // downstream push (if active)

  var bridge : Null<Output<Dynamic>>; // for binds
  //var bound : Null<Push<Dynamic>>; // for cleanup lifecycle when detaching nodes from a stale bind

  var id : Int = Flow.id();
  var name : Null<Dynamic>;
  var rank : Int = 0;
  var frame : Int = 0;
  var queued : Bool = false;

  var val : Maybe<A> = Nothing;
  var error : Null<Dynamic>;
  var ended : Bool = false;

  var next : Null<Push<Dynamic>>;
  var prev : Null<Push<Dynamic>>;

  function new(?name, ?ns : Array<Push<Dynamic>>, d) {
    def = d;
    if(ns != null) on = ns.copy(); // weakref?
    if(name != null) this.name = name;
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

  function run(F : Flow) {
    if(frame == F.frame) return;                    // already ran this node?
    frame = F.frame;                                // Mark ran

    if(!active()) return;                           // Skip the work, nobody is listening
    //trace("run ", [for (n in on.opt()) n.ok()], def);

    switch(def) {
      case From(_):  {}
      case Into(_), ApplyN(_), ApplyAsync(_), Const(_), Bind(_):
        if(on.ok() && on.foreach(n -> n.ok()))      // all dependencies are already propagated, check for ends
          switch(def) {
            case Into(f):   F.into(this, cast f, extract(cast on[0].val));
            case Const(x): resume(Val(x));
            case ApplyN(f):
              try{
                var as = [for(a in on) extract(cast a.val)]; //trace(as);
                var b = (cast f)(as); //trace(b);
                resume(Val(b));
              } catch (e : Dynamic) {
                resume(Error(e));
              }
            case ApplyAsync(f):
              (cast f)([for(n in on) extract(cast n.val)],
                       err -> F.resume(this, Error(err)),
                       v   -> F.resume(this, Val(v)));

            case Bind(f): {
              // Rewire the graph by redirecting future mb pushes to our outputs
              // and detaching the old mb
              if (to == null) trace("?? inactive bind");

              var a : Dynamic = cast extract(cast on[0].val);
              var mb : View<Dynamic> = (cast f)(a);
              //trace("bridging ", mb.node.def);

              if (bridge != null) {
                for (n in bridge.node.on.assume())
                  F.detach(n, bridge.node); // flipped arg order?
              }

              // this approach is dumber, but easier to reason about non-trivial dags
              bridge = Origin.on(null, mb, b -> { // activates too soon? no, see above assert
                //trace("bridge ", b, def);
                F.resume(this, Val(b));
              });

              // sophisticated approach:
              //   cross on control (lookat)
              //   x on cross, a, b
              //   bind ties cross to b
              //     reading cross reads b (need val getter)
              //     pushing b pushes cross (b.to.append(cross.to))
            }

            default: {}
          }
        else if(on.opt().exists(n -> n.ended))      // if my inbound edges are ended, end me
          resume(End);
        try{
          for(x in on.opt()) if(x.error != null)
             throw x.error; // break
        }
        catch(e : Dynamic){
          resume(Error(e));
        }
     }
  }

  function resume(a : Action<A>) {
    //trace("resume ", a, def);
    switch(a) {
      case Val(v):   val = Just(v);
      case Error(e): error = e;
      case End:      ended = true;
    }
    plan();
  }

  function plan() {
    //trace("plan ", def, to);
    if(to == null) return;
    to.sort((a, b) -> a.rank - b.rank);
    var a : Push<Dynamic> = this;
    for(b in to) {
      if(b.queued) continue;
      a.link(b);
      a = b;
    }
  }

  function link(a : Push<Dynamic>) {
    if(a == next) return;
    a.shift();
    a.next = next;
    a.prev = this;
    next = a;
  }

  function shift() {
    if(prev != null) prev.next = next;
    if(next != null) next.prev = prev;
    var b = next;
    prev = null; next = null;
    return b;
  }
}
