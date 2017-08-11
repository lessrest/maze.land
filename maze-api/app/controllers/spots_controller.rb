class SpotsController < ApplicationController
  def create
    maze = Maze.find(params.require(:maze))
    clip = Clip.find(params.require(:clip))
    name = params.require(:name)
    spot = Spot.create! \
      maze: maze,
      clip: clip,
      name: name
    render json: spot, status: 200
  end
end
