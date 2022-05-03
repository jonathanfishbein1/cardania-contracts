const webpack = require('webpack');
const NodePolyfillPlugin = require('node-polyfill-webpack-plugin');
const ModuleScopePlugin = require('react-dev-utils/ModuleScopePlugin');

module.exports = function (webpackConfig) {

    const wasmExtensionRegExp = /\.wasm$/;
    webpackConfig.resolve.extensions.push('.wasm');
    webpackConfig.experiments = {
        asyncWebAssembly: false,
        lazyCompilation: true,
        syncWebAssembly: true,
        topLevelAwait: true,
    };
    webpackConfig.resolve.fallback = {
        buffer: require.resolve('buffer/')
    }
    webpackConfig.module.rules.forEach((rule) => {
        (rule.oneOf || []).forEach((oneOf) => {
            if (oneOf.type === "asset/resource") {
                oneOf.exclude.push(wasmExtensionRegExp);
            }
        });
    });
    webpackConfig.plugins.push(new webpack.ProvidePlugin({
        Buffer: ['buffer', 'Buffer'],
    }));

    webpackConfig.resolve.modules.push("lib")
    webpackConfig.resolve.fallback = {
        "fs": false,
        "tls": false,
        "net": false,
        "path": false,
        "zlib": false,
        "http": false,
        "https": false,
        "stream": false,
        "crypto": false,
        "crypto-browserify": require.resolve('crypto-browserify'), //if you want to use this module also don't forget npm i crypto-browserify 
      };

    webpackConfig.plugins.push(
        new NodePolyfillPlugin()
    );

    webpackConfig.resolve.plugins = webpackConfig.resolve.plugins.filter(plugin => !(plugin instanceof ModuleScopePlugin));

    console.log(webpackConfig)
    

    return webpackConfig;
}