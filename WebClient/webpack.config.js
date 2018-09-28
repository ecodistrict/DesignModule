const path = require('path');
/* jshint unused:false */
const webpack = require('webpack');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = env => {
    const devMode = !env || !env.production;

    return {
        entry: './src/app.js',
        output: {
            filename: 'assets/js/[chunkhash].[name].bundle.js',
            path: path.resolve(__dirname, 'dist')
        },
        module: {
            rules: [
                // html-loader
                { 
                    test: /\.html$/,
                    use: ['html-loader']
                },
                // css-loader
                { 
                    test: /\.css$/, 
                    use: [
                        MiniCssExtractPlugin.loader,
                        {
                            loader: 'css-loader',
                            options: {
                                sourceMap: devMode,
                                url: false,
                                import: false
                            }
                        }                    
                    ]
                },
                // file-loader
                {
                    test: /\.(jpg|png|gif|svg)$/,
                    use: [
                        {
                            loader: 'file-loader',
                            options: {
                                name: '[name].[ext]',
                                outputPath: './assets/media/'
                            }
                        }
                    ]
                }
            ]
        },
        plugins: [
            new CleanWebpackPlugin(['dist']),
            new HtmlWebpackPlugin({
                template: 'index.html'
            }),
            new CopyWebpackPlugin([
                { from: 'Scripts', to: 'Scripts' },
                { from: 'lib', to: 'lib' },
                { from: 'Content', to: 'Content' },
                { from: 'bower_components', to: 'bower_components' },
                { from: 'favicon', to: 'favicon' },
                { from: 'portal', to: 'portal' }
            ]),
            new MiniCssExtractPlugin({
                filename: 'assets/style/[name].[hash].css',
                chunkFilename: 'assets/style/[id].[hash].css'
            }),
            new webpack.DefinePlugin({
                PRODUCTION: !devMode
            })
        ],

        devtool: devMode ? 'source-map' : false,
        mode: devMode ? 'development' : 'production'
    };
};
