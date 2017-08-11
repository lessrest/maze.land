class DontUseTypeAsColumnName < ActiveRecord::Migration[5.1]
  def change
    rename_column :clipfiles, :type, :kind
  end
end
