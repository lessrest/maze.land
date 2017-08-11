class AddClipIdToClipfile < ActiveRecord::Migration[5.1]
  def change
    add_reference :clipfiles, :clip, foreign_key: true
  end
end
