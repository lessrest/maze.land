class AddPathToClipfile < ActiveRecord::Migration[5.1]
  def change
    add_column :clipfiles, :path, :string
  end
end
