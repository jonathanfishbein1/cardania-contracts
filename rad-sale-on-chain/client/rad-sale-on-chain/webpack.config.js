import path from 'path'
import HtmlWebPackPlugin from "html-webpack-plugin"
import webpack from 'webpack'
const __dirname = path.resolve()

export default {
    mode: 'development',
    entry: './rad-sale-on-chain/index.ts',
    output: {
        path: path.resolve(__dirname, 'dist'),
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
                test: /\.css$/i,
                use: ["style-loader", "css-loader"],
            },
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
            }
        ]
    },
    plugins: [new HtmlWebPackPlugin({
        title: 'index',
        filename: `index.html`,
        template: `./rad-sale-on-chain/index.html`,
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