// Test that when electric calls scope.fn(), fn is called with `scope` bound as `this`, thus returning `"value"`
// See `js_calls_test.cljs`

// How to reproduce from JS console:

// ```js
// scope.fn(); // => "value"
// var fn = scope.fn;
// fn(); // => undefined

// fn.bind(scope)(); // => "value
// ```

export var scope = {
    value: "value",
    fn: function(){
        return this.value;
    }
};

export function install(){
    globalThis.hyperfiddle.js_calls_test_de.scope = scope;
}
