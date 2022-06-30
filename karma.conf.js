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
