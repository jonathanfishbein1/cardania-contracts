const express = require("express"),
    app = express()

app.use(express.static('./'))

app.listen(8080, () => {
    console.log("Listen on the port 8080...");
});