module.exports = {
    // mode: "jit"     // fast incremental build https://tailwindcss.com/docs/just-in-time-mode#watch-mode-and-one-off-builds
    prefix: 'tw-' // namespace
    , purge: { // filter all classes not in these files
        enabled: true
        , preserveHtmlElements: true
        , content: [
            './src/**/*.{cljc,cljs,js}'
            , './resources/public/**/*.{html,svg}'
        ]
    }
    , darkMode: false // or 'media' or 'class'
    , theme: {
        extend: {}
    }
    , variants: {
        extend: {
            textColor: ['visited']
        }
    }
    , plugins: []
}
