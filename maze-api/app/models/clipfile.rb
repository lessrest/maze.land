class Clipfile < ApplicationRecord
  belongs_to :clip

  def mime
    case kind
    when "MP4 H264 Vorbis"
      return "video/mp4"
    when "JPEG"
      return "image/jpeg"
    else
      return "application/octet-stream"
    end
  end
end
