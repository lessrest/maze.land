// Our Elastic Transcoder pipeline identifier
const PipelineId = "1469266154852-qh03z6"

let AWS = require ("aws-sdk")
let API = new AWS.ElasticTranscoder ({
  apiVersion: "2012-09-25",
  region: "eu-west-1",
})

exports.main = (event, context) => {
  let {
    bucket: { name },
    object: { key },
  } = event.Records[0].s3

  let kind = (prefix, ext, PresetId) => ({
    PresetId,
    Key: `${prefix}/clip.${ext}`,
    ThumbnailPattern: `${prefix}/thumbnail-{count}`,
  })

  let OutputKeyPrefix = `clip/${key.replace(/.*\//, "")}/`

  let params = {
    PipelineId, OutputKeyPrefix, Input: {
      Key: key,
      FrameRate: "auto",
      Container: "auto",
      Resolution: "auto",
      Interlaced: "auto",
      AspectRatio: "auto",
    },

    Outputs: [
      kind ("480p-h264",  "mp4",  "1502735837114-57b0hy"),
      kind ("720p-h264",  "mp4",  "1502735375914-bord97"),
      kind ("1080p-h264", "mp4",  "1502735339073-a5jkkx"),
      kind ("480p-vp9",   "webm", "1502735206108-08xagq"),
      kind ("720p-vp9",   "webm", "1502735229920-zz2pnr"),
      kind ("1080p-vp9",  "webm", "1502735187343-jmmstb"),
    ],
  }

  API.createJob (params, (error, data) => {
    console.log     (error || data)
    context.succeed ("started transcoder")
  })
}
