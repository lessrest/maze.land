class CreateDoors < ActiveRecord::Migration[5.1]
  def change
    create_table :doors do |t|
      t.references :src, foreign_key: { to_table: :spots }
      t.references :dst, foreign_key: { to_table: :spots }
    end
  end
end
