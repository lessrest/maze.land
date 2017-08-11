class AddClipToSpot < ActiveRecord::Migration[5.1]
  def change
    add_reference :spots, :clip, foreign_key: true
  end
end
