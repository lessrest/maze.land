class DontUseHashAsColumnName < ActiveRecord::Migration[5.1]
  def change
    rename_column :clipfiles, :hash, :sha2
  end
end
