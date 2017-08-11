class CreateMazes < ActiveRecord::Migration[5.1]
  def change
    create_table :mazes do |t|
      t.string :name

      t.timestamps
    end
  end
end
