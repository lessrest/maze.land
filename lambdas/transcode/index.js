let pipeline = "1469266154852-qh03z6"
let region = "eu-west-1"
let apiVersion = "2012-09-25"
let aws = require("aws-sdk")
let s3 = new aws.S3({ apiVersion })
let tc = new aws.ElasticTranscoder({ apiVersion, region })
exports.main = (event, context) => {
  let {
    bucket: { name },
    object: { key },
  } = event.Records[0].s3

  let output = (name, preset) => ({
    Key: name,
    ThumbnailPattern: "",
    PresetId: preset,
  })

  let params = {
    PipelineId: pipeline,
    OutputKeyPrefix: `clip/${key.replace(/.*\//, "")}/`,
    Input: {
      Key: key,
      FrameRate: "auto",
      Resolution: "auto",
      AspectRatio: "auto",
      Interlaced: "auto",
      Container: "auto",
    },
    Outputs: [
      output("480p.mp4", "1351620000001-000030"),
      output("720p.mp4", "1351620000001-000010"),
      output("1080p.mp4", "1351620000001-000001"),
      output("480p-vp9.webm", "1502449655212-5pnt8l"),
      output("720p-vp9.webm", "1502449629729-prnp17"),
      output("1080p-vp9.webm", "1502449552590-rorqw7"),
    ],
  }

  tc.createJob(params, (error, data) => {
    console.log(error || data)
    context.succeed("started transcoder")
  })
}
