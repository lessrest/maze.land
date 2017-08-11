class Maze < ApplicationRecord
  include FriendlyId
  friendly_id :name, use: [:slugged]
  has_many :spots
end
