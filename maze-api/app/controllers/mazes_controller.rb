class MazesController < ApplicationController
  def create
    maze = Maze.create!(name: params.require(:name))
    render json: { id: maze.id }, status: 200
  end

  def index
    mazes = Maze.all
    render \
      json: mazes,
      include: {
        spots: {
          include: {
            clip: {
              include: :clipfiles
            }
          }
        }
      },
      status: 200
  end
end
