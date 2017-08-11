class MazesController < ApplicationController
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
