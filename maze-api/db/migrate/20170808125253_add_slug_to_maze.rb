class AddSlugToMaze < ActiveRecord::Migration[5.1]
  def change
    add_column :mazes, :slug, :string, unique: true
    add_index :mazes, :slug
  end
end
