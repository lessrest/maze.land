class TranscodeJob < ApplicationJob
  queue_as :default

  def perform(path, clip_id)
    clip = Clip.find clip_id

    transcode clip,
      kind: "Original",
      path: path,
      cmd: ["true"]

    transcode clip,
      kind: "MP4 H264 Vorbis",
      path: path + ".mp4",
      cmd: [
        "avconv", "-i", path,
        "-c:v:0", "libx264", "-preset", "slower", "-crf", "30",
        "-c:a:0", "libvorbis", "-qscale:a", "5", "-ar", "48000",
        "-loglevel", "error",
        path + ".mp4"
      ]

    transcode clip,
      kind: "JPEG",
      path: path + ".jpg",
      cmd: [
        "avconv", "-i", path,
        "-vframes", "1", path + ".jpg",
        "-loglevel", "error",
      ]
  end

  private
  def transcode(clip, options)
    puts "Transcoding `#{clip.name}' into #{options[:kind]}"
    out = options[:path]
    system(*options[:cmd])
    x = Clipfile.create!(
      sha2: IO.popen(["sha256sum", out]).read.split(" ").first,
      size: IO.popen(["stat", "-c", "%s", out]).read.split(" ").first,
      kind: options[:kind],
      clip: clip,
      path: out
    )
  end
end
