package hyperfiddle.hx;
import hyperfiddle.hx.Meta;
using Lambda;
using hyperfiddle.hx.Meta.X;

@:expose("Origin")
@:publicFields class Origin {
  static var main : Flow;
  static function get() return if(main != null) main else main = new Flow();  // Singleton
  static function all(f) get().all(f);  // Flow.all

  static var onError : Error -> Void = (error) -> trace(error);

  static function input<A>(?f) {  // f is double function f(). First invocation is for start, second for end
    return new Input<A>(get(), new Push(From({
      var end = null;
      { on: () -> if(f != null) end = f(),
        off: () -> if(end != null) {end(); end = null;} } })));
  }

  static function on<A>(v : View<A>, f : A -> Void) {
    var output = new Output(get(), new Push([v.node], Into(f)));
    output.init();
    return output;
  }

  static function pure<A>(a : A) {
    var output = new Output(get(), new Push([], Constant(a)));
    output.init();
    return output;
  }

  static function apply(ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push([for(x in ns) x.node],
      switch(ns.length) {
        case 1: Apply(f);
        case 2: Apply2(f);
        case 3: Apply3(f);
        case 4: Apply4(f);
        case 5: Apply5(f);
        default: throw new Error('can\'t apply $ns');
      })
    );
  }

  static function reduce<A, B>(v : View<Dynamic>, initial : Dynamic, f : (A, A) -> B) {
    var node: Push<Dynamic> = new Push([v.node], Reduce(f));
    node.last = initial;
    return new View(get(), node);
  }

  static function filter<A>(v : View<Dynamic>, f : A -> Bool) {
    return new View(get(), new Push([v.node], Filter(f)));
  }
}

@:expose("View")
@:publicFields class View<A> {
  var F : Flow;
  var node : Push<A>;
  function end() {F.put(node, End);}
  function new(f, n) {F = f; node = n;}
}

@:expose("Input")
@:publicFields class Input<A> extends View<A> {
  function put(a : A) {F.put(node, Val(a));}
}

@:expose("Output")
@:publicFields class Output<A> extends View<A> {
  function init() {F.update(node);}
}

@:expose("NodeDef")
enum NodeDef<T> {
  Constant<A>(a : A) : NodeDef<A>;
  From<A>(s : {on : () -> Void, off : () -> Void}) : NodeDef<A>;
  Into<A>(f : A -> Void);
  Reduce<A,B>(f : (A, A) -> B) : NodeDef<B>;
  Filter<A>(f : A -> Bool) : NodeDef<A>;
  Apply<A, B>(f : A -> B) : NodeDef<B>;
  Apply2<A, B, C>(f : (A, B) -> C) : NodeDef<C>;
  Apply3<A, B, C, D>(f : (A, B, C) -> D) : NodeDef<D>;
  Apply4<A, B, C, D, E>(f : (A, B, C, D) -> E) : NodeDef<C>;
  Apply5<A, B, C, D, E, F>(f : (A, B, C, D, E) -> F) : NodeDef<D>;
}

@:expose("Action")
enum Action<A> {
  Val(a : Null<A>);
  Error(e : Error);
  End;
}

typedef Frame = Int;
typedef Rank = Int;

@:expose("Flow")
@:nullSafety(Loose)
@:publicFields class Flow {
  static var count = 0;
  static function id() {return ++count;}

  var lock : Bool = false;
  var frame : Frame = 0;
  var queue : Array<Array<Push<Dynamic>>> = [];

  function new() {}

  function put<A>(node : Push<A>, a : Action<A>) {
    node.put(a);
    var node = node.unlink();
    if(node != null) add(node);
    run();
  }

  function all(f : Void -> Void) {
    lock = true;
    var e : Null<Error> = null;
    try f() catch(x : Error) e = x;
    lock = false;
    run();
    onError(e);
  }

  function add(node : Push<Dynamic>) {
    if(node.queued) return;
    while(queue.length <= node.rank) queue.push([]);
    queue[node.rank].push(node);
    node.queued = true;
  }

  function run() {
    if(lock) return;
    lock = true;

    frame++;

    var e : Null<Error> = null;
    try {
      var rank = 0;

      while(rank < queue.length) {
        for(node in queue[rank])
          node.run(this);

        for(node in queue[rank]) {
          var node = node.unlink();
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
    catch(x : Error) e = x;
    lock = false;
    onError(e);
  }

  function into<A>(n : Push<A>, f : A -> Void, val : A) {
    f(val);
  }

  function onError(?e) {
    if(e != null)
      Origin.onError(e);
  }

  function clear(n : Push<Dynamic>) {
    switch(n.def) {
      case Constant(_):

      default:
        if(!n.queued) return;
        n.queued = false;

        if(!n.to.opt().exists(x -> x.joins()))
          n.val = null;

        for(x in n.on.opt()) clear(x);
    }
  }

  function update(a : Push<Dynamic>) {
    if(!a.active()) return;
    a.rank = 0;
    for(x in a.on.opt()) {
      attach(x, a);
      if(x.rank > a.rank) a.rank = x.rank;
    }
    if(a.joins()) a.rank++;
  }

  function attach(a : Push<Dynamic>, b : Push<Dynamic>) {
    if(a.to == null) {
      a.to = [b];
      switch(a.def) {
        case From(source):
          if(source.on != null) source.on();
        default:
      }
    }
    else if(!a.to.has(b))
      a.to.push(b);
    update(a);
  }

  function detach(a : Push<Dynamic>, b : Push<Dynamic>) {
    if(a.to.ok() && a.to.remove(b)) {
      if(a.to.nil())
        switch(a.def) {
          case From(source):
            if(source.off != null) source.off();
          default:
            for (x in a.on.opt()) detach(x, a); // ...
        }
    }
  }
}

@:expose("Push")
@:nullSafety(Loose)
@:publicFields class Push<A> {
  var def : NodeDef<A>;
  var on : Null<Array<Push<Dynamic>>>;
  var to : Null<Array<Push<Dynamic>>>;

  var id : Int = Flow.id();
  var rank : Rank = 0;
  var frame : Frame = 0;
  var queued : Bool = false;

  var val : Null<A>;
  var error : Null<Dynamic>;
  var ended : Bool = false;
  var last : Null<A>;

  var next : Null<Push<Dynamic>>;
  var prev : Null<Push<Dynamic>>;

  function new(?ns : Array<Push<Dynamic>>, d) {
    def = d;
    switch (def) {
      case Constant(x):
        queued = true;
        val = x;
        last = x;
      default:
    }
    if(ns != null) on = ns.copy();
  }

  function toString() return 'Push($id, $rank, $def)';

  function ok()     return val != null && !ended;
  function active() return !ended && (to != null || def.match(Into(_)));
  function joins()  return on.ok() && on.length > 1;

  function run(F : Flow) {
    if(frame == F.frame) return;
    frame = F.frame;

    if(!active()) return;

    try switch(def) {
      case From(_), Constant(_):  {}
      case Into(_), Apply(_), Apply2(_), Apply3(_), Apply4(_), Apply5(_),
           Reduce(_), Filter(_):
        if(on.ok() && on.foreach(n -> n.ok()))
          switch(def) {
            case Into(f):   F.into(this, cast f, cast on[0].val);
            case Reduce(f): put(Val((cast f)(last, on[0].val)));
            case Filter(f): if ((cast f)(on[0].val)) put(Val(on[0].val));
            case Apply(f): put(Val((cast f)(on[0].val)));
            // {
            //   var arr : Dynamic = new Array();
            //   for (x in on.opt()) arr.push(x.val);
            //   Reflect.callMethod(this, f, arr);
            // }
            case Apply2(f): put(Val((cast f)(on[0].val, on[1].val)));
            case Apply3(f): put(Val((cast f)(on[0].val, on[1].val, on[2].val)));
            case Apply4(f): put(Val((cast f)(on[0].val, on[1].val, on[2].val, on[3].val)));
            case Apply5(f): put(Val((cast f)(on[0].val, on[1].val, on[2].val, on[3].val, on[4].val)));
            default:        {}
          }
        else if(on.opt().exists(n -> n.ended))
          put(End);
        for(x in on.opt()) if(x.error != null)
          throw x.error;
    }
    catch(e : Error)
      put(Error(e));
  }

  function put(a : Action<A>) {
    switch(a) {
      case Val(v):   val = v; last = v;
      case Error(e): error = e;
      case End:      ended = true;
    }
    push();
  }

  function push() {
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
