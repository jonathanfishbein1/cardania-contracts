const express = require("express"),
    app = express()

app.use(express.static('./dist'))

app.listen(8080, () => {
    console.log("Listen on the port 8080...");
});