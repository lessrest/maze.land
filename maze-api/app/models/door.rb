class Door < ApplicationRecord
  belongs_to :src, class_name: "Spot"
  belongs_to :dst, class_name: "Spot"
end