/*

The ONLY interesting thing in this file is that the entry is changed
  from './src/index.js' to './src/index.bs.js'
Everything else comes from the default webpack.config.js found on
  https://webpack.js.org/guides/getting-started/#using-a-configuration

 */

const path = require('path');

module.exports = {
  entry: './src/index.mjs',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
    ],
    // plugins: [
    //   new webpack.DefinePlugin({
    //     "process.env.NODE_ENV": JSON.stringify("production")
    //   })
    // ]
  },
  watch: true,
};