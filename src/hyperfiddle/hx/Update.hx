package hyperfiddle.hx;

using Lambda;

class Update {
  public static var F : Flow;

  public static function get() {
    if(F == null) F = new Flow();
    return F;
  }

  public static function all(f) {
    get().all(f);
  }

  public static function input<A>(?f) {
    return new Input<A>(get(), f, new Push(From));
  }

  public static function on<A>(v : View<A>, f : A -> Void) {
    return new Output(get(), new Push([v.node], Into(f))).init();
  }

  public static function apply(ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push([for(x in ns) x.node],
      switch(ns.length) {
        case 1: Apply(f);
        case 2: Apply2(f);
        case 3: Apply3(f);
        default: throw new Error('cant apply $ns');
      })
    );
  }
}

@:publicFields class View<A> {
  var F : Flow;
  var node : Push<A>;
  function new(f, n) {F = f; node = n;}
}

@:publicFields class Input<A> extends View<A> {
  var notify : () -> Null<() -> Void>;
  function new(f, ?nf, n) {super(f, n); notify = nf;}
  function put(a : A) {F.push(a, node);}
  function end() {F.end(node);}
}

@:publicFields class Output<A> extends View<A> {
  function init() {node.update();}
  function off() {F.end(node);}
}

#if java   typedef Error = java.lang.Exception;
#elseif js typedef Error = js.lib.Error;
#else      typedef Error = {}; #end

typedef Frame = Int;
typedef Rank = Int;

enum NodeDef<T, R> {
  From<A> : NodeDef<{}, A>;
  Into<A>(f : A -> Void);
  Apply<A, B>(f : A -> B) : NodeDef<A -> B, B>;
  Apply2<A, B, C>(f : (A, B) -> C) : NodeDef<(A, B) -> C, C>;
  Apply3<A, B, C, D>(f : (A, B, C) -> D) : NodeDef<(A, B, C) -> D, D>;
}

@:publicFields private class Flow {
  var lock : Bool;
  var frame : Frame = 0;
  var queue : Array<Array<Push<Dynamic>>> = [];

  function new() {}

  function push<A>(a : A, node : Push<A>) {
    node.val = a;
    add(node);
    update();
  }

  function end(node : Push<Dynamic>) {
    node.end();
    add(node);
    update();
  }

  function add(node : Push<Dynamic>) {
    if(node.queued) return;
    while(queue.length <= node.rank) queue.push([]);
    queue[node.rank].push(node);
    node.queued = true;
  }

  function all(f : Void -> Void) {
    lock = true;

    var e = null;
    try f() catch(x : Any) e = x;

    lock = false;
    if(e != null) throw e;

    update();
  }

  function update() {
    if(lock) return;
    lock = true;

    frame++;

    var e = null;
    try {
      var rank = 0;

      while(rank < queue.length) {
        for(node in queue[rank])
          node.run(this);

        for(node in queue[rank]) {
          node = node.unlink();
          while(node != null) {
            if(node.rank == rank)
              node.run(this);
            else if(node.rank > rank)
              add(node);
            node = node.unlink();
          }
        }

        for(node in queue[rank])
          clear(node);

        queue[rank].resize(0);

        rank++;
      }
    }
    catch(x : Any) {e = x;}
    lock = false;
    if(e != null) throw e;
  }

  function into<A>(n : Push<A>, f : A -> Void) {
    f(n.on[0].val);
  }

  function clear(n : Push<Dynamic>) {
    if(!n.queued) return;
    n.queued = false;
    if(!n.to.exists(x -> x.isJoin())) {
      n.val = null;
      if(n.on != null)
        for(x in n.on) clear(x);
    }
  }
}

@:publicFields private class Push<A> {
  var def : NodeDef<Dynamic, A>;
  var on : Array<Push<Dynamic>>;
  var to : Array<Push<Dynamic>>;

  var prop : Bool;
  var ended : Bool;
  var error : Dynamic;

  var id : Int = getId();
  var rank : Rank = 0;
  var frame : Frame = 0;
  var queued : Bool;
  var val : Null<A>;

  var next : Push<Dynamic>;
  var prev : Push<Dynamic>;

  static var count = 0;
  static function getId() {return ++count;}

  function new(?ns : Array<Push<Dynamic>>, d) {
    def = d;
    if(ns != null) on = ns.copy();
  }

  function toString() {
    return 'Push($id, $rank, $def)';
  }

  function attach(n) {
    if(to == null) to = [];
    if(!to.has(n)) to.push(n);
    update();
  }

  function detach(n) {
    switch(to.indexOf(n)) {
      case -1: return;
      case ix: to.splice(ix, 1);
    }
    if(to.length == 0) to = null;
    update();
  }

  function ok() {return !ended && frame > 0;}
  function active() {return switch(def) {case Into(_): true; default: to != null;}}
  function isJoin() {return on.length > 1;}

  function update() {
    switch(active()) {
      case true:
        if(on == null) return;
        rank = 0;
        for(x in on) x.attach(this);
        for(x in on) if(x.rank > rank) rank = x.rank;
        if(isJoin()) rank++;
      case false:
        if(on == null) return;
        for(x in on) x.detach(this); // ...
    }
  }

  //  function push(from : Push<Dynamic>, val : Dynamic) {}

  function run(F : Flow) {
    if(frame == F.frame) return;
    frame = F.frame;

    if(!active()) return;

    try switch(def) {
      case From:
        put(val);
      case Into(_), Apply(_), Apply2(_), Apply3(_):
        if(on.foreach(n -> n.ok())) {
          switch(def) {
            case Into(f):   F.into(this, f);
            case Apply(f):  put(f(on[0].val));
            case Apply2(f): put(f(on[0].val, on[1].val));
            case Apply3(f): put(f(on[0].val, on[1].val, on[2].val));
            default: {}
          } }
        else if(on.exists(n -> n.ended))
          end();
        switch(on.find(n -> n.error != null)) {
          case e if(e != null): error = e;
          default: {}
        }
    }
    catch(e : Any) {error = e;}

    if(error != null) end();
  }

  function put(a : A) {val = a; forward();}
  function end() {ended = true; forward();}

  function forward() {
    if(to == null) return;
    var n : Push<Dynamic> = this;
    for(x in to) {
      if(x.queued) continue;
      n.link(x);
      n = x;
    }
  }

  function link(n : Push<Dynamic>) {
    if(n == next) return;
    n.unlink();
    n.next = next;
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
