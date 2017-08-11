class CreateClipfiles < ActiveRecord::Migration[5.1]
  def change
    create_table :clipfiles do |t|
      t.string :hash
      t.integer :size
      t.string :type

      t.timestamps
    end
  end
end
