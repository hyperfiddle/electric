process.env.CHROMIUM_BIN = require('puppeteer').executablePath()
module.exports = function (config) {
    config.set({
        browsers: ['ChromiumHeadless'],
        // The directory where the output file lives
        basePath: 'out',
        // The file itself
        files: ['karma-tests.js'],
        frameworks: ['cljs-test'],
        plugins: ['karma-cljs-test', 'karma-chrome-launcher'],
        colors: true,
        logLevel: config.LOG_INFO,
        client: {
            args: ["shadow.test.karma.init"],
            singleRun: true
        }
    });
};
