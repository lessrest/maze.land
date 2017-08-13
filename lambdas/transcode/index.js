// Our Elastic Transcoder pipeline identifier
const PipelineId = "1469266154852-qh03z6"

let AWS = require ("aws-sdk")
let API = new aws.ElasticTranscoder ({
  apiVersion: "2012-09-25",
  region: "eu-west-1",
})

exports.main = (event, context) => {
  let {
    bucket: { name },
    object: { key },
  } = event.Records[0].s3

  let kind = (Key, PresetId) => ({
    Key, PresetId, ThumbnailPattern: "",
  })

  let OutputKeyPrefix = `clip/${key.replace(/.*\//, "")}/`

  let params = {
    PipelineId, OutputKeyPrefix, Input: {
      Key: key,
      FrameRate:   "auto",
      Container:   "auto",
      Resolution:  "auto",
      Interlaced:  "auto",
      AspectRatio: "auto",
    },

    Outputs: [
      kind ("480p-h264.mp4",  "1351620000001-000030"),
      kind ("720p-h264.mp4",  "1351620000001-000010"),
      kind ("1080p-h264.mp4", "1351620000001-000001"),
      kind ("480p-vp9.webm",  "1502449655212-5pnt8l"),
      kind ("720p-vp9.webm",  "1502449629729-prnp17"),
      kind ("1080p-vp9.webm", "1502449552590-rorqw7"),
    ],
  }

  API.createJob (params, (error, data) => {
    console.log     (error || data)
    context.succeed ("started transcoder")
  })
}
