package hyperfiddle.hx;
// import haxe.macro.Expr;
// import haxe.macro.Context;
// import haxe.macro.Compiler;

typedef Error = haxe.Exception;

@:publicFields class Meta {
  static var target =
    #if macro Context.definedValue('target.name')
    #else null #end;
}

@:publicFields class X {
  static inline function ok<A>(x : Null<A>) { return x != null; }
  static inline function assume<A>(x : Null<A>) : A { return cast x; }
  static inline function check(x : Null<Error>) { if(x != null) throw x; }
  static inline function nil<A>(x : Null<Array<A>>) { return x == null || x.length == 0; }
  static inline function opt<A>(x : Null<Array<A>>) : Iterable<A> { return if(x != null) x else []; }
}
