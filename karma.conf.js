process.env.CHROME_BIN = require('puppeteer').executablePath()
module.exports = function (config) {
    config.set({
        browsers: ['Chrome_CI'],
        // The directory where the output file lives
        basePath: 'out',
        // The file itself
        files: ['karma-tests.js'],
        frameworks: ['cljs-test'],
        plugins: ['karma-cljs-test', 'karma-chrome-launcher'],
        colors: true,
        logLevel: config.LOG_INFO,
        // The synchronous Electric reactor tests block Chrome's main thread in bursts,
        // starving karma's heartbeat; the 2s ping default kills the browser mid-run
        // (verified: default timeouts disconnect at "0 of 369 executed", ping timeout).
        browserNoActivityTimeout: 120000,
        browserDisconnectTimeout: 60000,
        pingTimeout: 60000,
        client: {
            args: ["shadow.test.karma.init"],
            singleRun: true
        },
        customLaunchers: {
            Chrome_CI: {
                base: 'ChromeHeadless',
                flags: ['--no-sandbox']
            }
        }
    });
};
