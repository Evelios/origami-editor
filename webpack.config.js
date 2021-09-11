var path = require("path");

module.exports = {
    mode: "none",
    entry: "./src/WebApplication/WebApplication.fsproj",
    devServer: {
        contentBase: path.join(__dirname, "./dist")
    },
    module: {
        rules: [{
            test: /\.fs(x|proj)?$/,
            use: "fable-loader"
        }]
    }
}
