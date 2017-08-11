class AddMazeToSpot < ActiveRecord::Migration[5.1]
  def change
    add_reference :spots, :maze, foreign_key: true
  end
end
