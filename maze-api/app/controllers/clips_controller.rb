class ClipsController < ApplicationController
  def index
    clips = Clip.all
    render json: clips, include: [:clipfiles, :spots], status: 200
  end

  def create
    clip = Clip.create!(name: params.require(:hash))
    render json: { id: clip.id }, status: 200
  end
end
