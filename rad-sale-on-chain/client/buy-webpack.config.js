import path from 'path'
import HtmlWebPackPlugin from "html-webpack-plugin"
import webpack from 'webpack';
const __dirname = path.resolve()
    , mode = process.env.NODE_ENV
console.log(mode)
export default {
    mode: mode,
    entry: './src/buy.ts',
    output: {
        path: path.resolve(__dirname, 'distBuy'),
        publicPath: '/',
        filename: '[contenthash].js',
        clean: true
    },
    target: 'web',
    resolve: {
        extensions: ['.tsx', '.ts', '.js'],
    },
    module: {
        rules: [
            {
                test: /\.html$/,
                use: [
                    {
                        loader: "html-loader",
                    }
                ]
            },
            {
                test: /\.tsx?$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
            {
                test: /\.elm$/,
                use: 'elm-webpack-loader',
                exclude: [/elm-stuff/, /node_modules/],
            }
        ],
        noParse: /\.elm$/
    },
    plugins: [new HtmlWebPackPlugin({
        title: 'index',
        filename: `index.html`,
        template: `./src/buy.html`,
    }),
    new webpack.DefinePlugin({
        MODE: JSON.stringify(mode)
    }),
    new webpack.ProvidePlugin({ Buffer: ['buffer', 'Buffer'] })
    ]
    , experiments: {
        asyncWebAssembly: true,
        outputModule: true,
        topLevelAwait: true,
        layers: true
    }
};