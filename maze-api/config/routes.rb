Rails.application.routes.draw do
  resources :spots
  resources :clipfiles
  resources :clips
  resources :mazes
  resources :doors

  get "/blobs/:sha2", to: "clipfiles#download"
end
