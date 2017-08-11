class ClipfilesController < ApplicationController
  def download
    file = Clipfile.find_by sha2: params[:sha2]
    send_file file.path, type: file.mime
  end
end
