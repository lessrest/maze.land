let form = require("formidable")
let http = require("http")
let util = require("util")
let exec = util.promisify(require("child_process").execFile)
let yell = console.log.bind(console)
let fs = require("fs")

let aws = require("aws-sdk")
let s3 = new aws.S3({
  apiVersion: "2006-03-01",
  params: {
    Bucket: process.env["MAZE_CLIP_BUCKET"],
  }
})

function post ({ host, path, json }) {
  return new Promise((pass, fail) => {
    let body = JSON.stringify(json)
    let req = http.request({
      hostname: host,
      port: 80,
      path: path,
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Content-Length": body.length
      },
    }, ({ statusCode }) => {
      if (statusCode !== 200) {
        fail()
      } else {
        pass()
      }
    })
    req.on("error", e => fail(e))
    req.write(body)
    req.end()
  })
}

async function sha256sum (file) {
  let { stdout } = await exec("sha256sum", [file.path])
  return stdout.split(" ")[0]
}

async function handle (req, res) {
  let path = `${req.method} ${req.url}`
  yell(`Request: ${path}`)
  if (path == "OPTIONS /") {
    res.writeHead(200, {
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "POST",
    })
    res.end()
  } else if (path == "POST /") {
    let x = new form.IncomingForm()
    x.maxFieldsSize = process.env["MAX_UPLOAD_SIZE"]
    x.parse(req, async (error, fields, files) => {
      try {
        let hashes = []
        if (error) {
          throw error
        } else {
          for (let file of Object.values(files)) {
            let hash = await sha256sum(file)
            hashes.push(hash)
            yell(`Uploading ${hash}`)
            await s3.putObject({
              Key: `original/${hash}`,
              ACL: "public-read",
              Body: fs.createReadStream(file.path),
              ContentType: "application/octet-stream",
            }).promise()
            yell(`Registering ${hash}`)
            await post({
              host: "maze-api",
              path: "/clips",
              json: { hash }
            })
            yell(`Done with ${hash}`)
          }
        }
        res.writeHead(200, {
          "content-type": "application/json"
        })
        res.end(JSON.stringify(hashes))
      } catch (e) {
        yell(e)
        res.writeHead(500)
        res.end()
      }
    })
  } else {
    res.writeHead(404, { "content-type": "text/plain" })
    res.end("Nope.\n")
  }
}

http.createServer((req, res) => {
  handle(req, res)
}).listen(80)

process.on("SIGTERM", () => {
  console.log("Stopping")
  process.exit()
})

console.log("Listening")