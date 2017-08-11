# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20170809151955) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "clipfiles", force: :cascade do |t|
    t.string "sha2"
    t.integer "size"
    t.string "kind"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.bigint "clip_id"
    t.string "path"
    t.index ["clip_id"], name: "index_clipfiles_on_clip_id"
  end

  create_table "clips", force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "doors", force: :cascade do |t|
    t.bigint "src_id"
    t.bigint "dst_id"
    t.index ["dst_id"], name: "index_doors_on_dst_id"
    t.index ["src_id"], name: "index_doors_on_src_id"
  end

  create_table "mazes", force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "slug"
    t.index ["slug"], name: "index_mazes_on_slug"
  end

  create_table "spots", force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.bigint "clip_id"
    t.bigint "maze_id"
    t.index ["clip_id"], name: "index_spots_on_clip_id"
    t.index ["maze_id"], name: "index_spots_on_maze_id"
  end

  add_foreign_key "clipfiles", "clips"
  add_foreign_key "doors", "spots", column: "dst_id"
  add_foreign_key "doors", "spots", column: "src_id"
  add_foreign_key "spots", "clips"
  add_foreign_key "spots", "mazes"
end
