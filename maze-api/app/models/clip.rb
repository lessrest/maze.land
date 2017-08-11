class Clip < ApplicationRecord
  has_many :clipfiles
  has_many :spots
end
