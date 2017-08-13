let form = require("formidable")
let http = require("http")
let util = require("util")
let exec = util.promisify(require("child_process").execFile)
let yell = console.log.bind(console)

let aws = require("aws-sdk")
let fs = require("fs")
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
      port:     80,
      path:     path,
      method:   "POST",
      headers:  {
        "Content-Type":  "application/json",
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
  let action = `${req.method} ${req.url}`
  yell(`Request: ${action}`)
  
  try {
    if (action == "OPTIONS /")
      await cors(res)
    else if (action == "POST /")
      await work(req, res)
    else throw "nope"
    
  } catch (e) {
    yell(e); res.writeHead(500); res.end()
  }
}

async function cors (res) {
  res.writeHead(200, {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Methods": "POST",
  })
  res.end()
}

let parseForm = req => {
  return new Promise((ok, no) => {
    let x = new form.IncomingForm()
    x.maxFieldsSize = process.env["MAX_UPLOAD_SIZE"]
    x.parse(req, (error, fields, files) => {
      if (error) no(error)
      else       ok({ fields, files })
    })
  })
}

async function work (req, res) {
  let hashes = []
  let { files } = await parseForm(req)
  
  for (let file of Object.values(files)) {
    let hash = await sha256sum(file)
    hashes.push(hash)
    
    yell(`Uploading ${hash} to S3`)
    await s3.putObject({
      ContentType: "application/octet-stream",
      Body:        fs.createReadStream(file.path),
      Key:         `original/${hash}`,
      ACL:         "public-read",
    }).promise()
    
    yell(`Registering ${hash} with API`)
    await post({
      host: "maze-api",
      path: "/clips",
      json: { hash }
    })
    
    yell(`Done with ${hash}`)
  }
  
  res.writeHead(200, { "content-type": "application/json" })
  res.end(JSON.stringify(hashes))
}

http.createServer((req, res) => {
  handle(req, res)
}).listen(80)

// Handle Docker Ctrl-C, etc.
process.on("SIGTERM", () => {
  console.log("Stopping")
  process.exit()
})

console.log("Listening")
