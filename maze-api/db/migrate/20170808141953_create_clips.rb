class CreateClips < ActiveRecord::Migration[5.1]
  def change
    create_table :clips do |t|
      t.string :name

      t.timestamps
    end
  end
end
