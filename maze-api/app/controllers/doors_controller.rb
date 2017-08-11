class DoorsController < ApplicationController
  def create
    src = Spot.find(params.require(:src))
    dst = Spot.find(params.require(:dst))
    door = Door.create! src: src, dst: dst
    render json: door, status: 200
  end

  def index
    render json: Door.all, status: 200
  end
end