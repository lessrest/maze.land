// Settings

let subdomain = x => [
  document.location.protocol,
  document.location.host.replace(/^www\./, x),
].join("//")

const apiRoot = subdomain("api.")
const putRoot = subdomain("put.")
const pgfRoot = subdomain("pgf.")
const cdn = "https://d2ayo97fkylvct.cloudfront.net"

const pgf = {
  completions: xs => {
    let x = encodeURIComponent(xs.join(" "))
    return fetch(
      `${pgfRoot}/Maze.pgf?command=complete&from=MazeEng&input=${x}+`
    ).then(x => x.json()).then(x => x[0].completions)
  }
}

// Framework

let get = (xs, id) => xs.filter(x => x.id == id)[0]

let pull = x => fetch(`${apiRoot}${x}`).then(x => x.json())
let push = (x, y) =>
  fetch(`${apiRoot}${x}`, {
    method: "POST",
    headers: {
      "Accept": "application/json",
      "Content-Type": "application/json",
    },
    body: JSON.stringify(y),
  }).then(x => x.json())

let yell = (...x) => console.info(...x)
let wait = (f, x) => setTimeout(f, x)
let loop = Symbol("loop")
let plan = x => new Promise(x)
let draw = x => ReactDOM.render(x, document.querySelector("#app"))
let doze = x => plan(f => wait(() => f(), x * 1000))
let pick = x => Promise.race(x.map(x => x.vow))
let nest = f => {
  let ok; return Object.assign(
    (...x) => f(...x).then(ok),
    { vow: plan(F => ok = F) }
  )
}

// XHR with progress stream

let upload = (file, path) => {
  let stream = {}
  let data = new FormData()
  data.append("file", file)

  let xhr = new XMLHttpRequest()
  xhr.upload.addEventListener(
    "progress",
    e => stream.resolve(e.loaded / file.size),
    false
  )
  xhr.upload.addEventListener(
    "load",
    () => stream.resolve(1),
    false
  )
  xhr.open("POST", `${putRoot}${path}`)
  xhr.send(data)

  return stream
}

let progress = stream => new Promise(resolve => stream.resolve = resolve)

// Maze app

let APP = async function() {
  draw(Spin("Loading mazes..."))
  let mazes = await pull("/mazes")
  await doze(0.5)
  draw(Spin("Loading clips..."))
  let clips = await pull("/clips")
  yell(clips)
  draw(Spin("Loading doors..."))
  let doors = await pull("/doors")
  await doze(0.5)
  await OVERVIEW({ mazes, clips, doors })
  yell("App exited")
}

wait(APP)

let OVERVIEW = async function ({
  mazes, clips, doors,
}) {

  let UPLOAD_CLIPS = nest(
    async function (
      files
    ) {
      for (let file of files) {
        let stream = upload(file, "/")
        while (true) {
          let ratio = await progress(stream)
          if (ratio === 1) {
            break
          } else {
            draw(Spin(`Uploading ${file.name}: ${(ratio * 100).toFixed(2)}...`))
          }
        }
        draw(Splash("Done!"))
        await doze(0.5)
      }
    }
  )

  let REFRESH_CLIPS = nest(
    async function () {
      draw(Spin("Refreshing clips..."))
      clips = await pull("/clips")
      await doze(0.5)
    }
  )

  let ADD_NODE = nest(
    async function ({
      node, callback, clip = clips[0], name = "",
    }) {

      let SELECT_CLIP = nest(
        async function (
          id
        ) {
          clip = get(clips, id)
          return loop
        }
      )

      let MAKE = nest(
        async function ({
          maze, clip
        }) {
          draw(Spin("Making new spot..."))
          await doze(0.5)
          await push(`/spots`, { maze, clip, name })
          draw(Spin("Refreshing mazes..."))
          mazes = await pull("/mazes")
        }
      )

      let CANCEL = nest(
        async function () {
          draw(Splash("Cancelled."))
          await doze(0.5)
          callback(null)
        }
      )

      let SET_NAME = nest(
        async function (x) {
          name = x
          return loop
        }
      )

      let Clip = clip => (
        <div className="clip">
          {
            <img
               style={{ width: "8rem" }}
               src={thumbnailUrl(clip)}
            />
          }
        </div>
      )

      let $maze

      draw(
        <div className="dialog">
          <section>
            <h1>New spot</h1>
            <form
              onSubmit={e => {
                e.preventDefault()
                MAKE({
                  maze: $maze.value,
                  clip: clip.id,
                })
             }}
            >
              <select ref={x => $maze = x}>
                {
                  mazes.map(
                    x => <option value={x.id}>{x.name}</option>
                  )
                }
              </select>
              <div className="clips">
                {
                  clips.length > 0
                    ? clips.map(x =>
                        <label className={x.id === clip.id ? "active" : ""}>
                          <input
                            type="radio"
                            name="clip"
                            value={x.id}
                            checked={x.id === clip.id}
                            onChange={e => SELECT_CLIP(e.target.value)} />
                          {Clip(x)}
                        </label>
                      )
                    : <span>You have no clips yet.</span>
                }
              </div>
              <input
                autoFocus
                placeholder="Spot name"
                value={name}
                onChange={x => SET_NAME(x.target.value)}
              />
              <div style={{ position: "relative" }}>
                <video
                  autoPlay
                  loop
                  src={videoSource(clip)}
                />
                { Menu({ name }) }
              </div>
              <span
                className="button"
                onClick={
                  () => MAKE({
                    maze: $maze.value,
                    clip: clip.id,
                  })
                }
              >
                Make spot
              </span>
              <span className="button" onClick={CANCEL}>
                Cancel
              </span>
            </form>
          </section>
        </div>
      )

      let x = await pick([MAKE, CANCEL, SELECT_CLIP, SET_NAME])
      if (x === loop) {
        await ADD_NODE({ node, callback, clip, name })
      }
    }
  )

  let ADD_EDGE = nest(
    async function ({
      src, dst,
    }) {
      draw(Spin("Saving edge..."))
      await push("/doors", { src: src.id, dst: dst.id })
      draw(Spin("Refreshing doors..."))
      doors = await pull("/doors")
    }
  )

  let ENTER_SPOT = nest(
    async function ({
      node, callback, maze
    }) {
      let spot = get(maze.spots, node.id)
      draw(
        <div className="game">
          <video
            autoPlay
            loop
            src={videoSource(spot.clip)}
          />
          { Menu({ name: spot.name }) }
        </div>
      )
      await doze(3)
    }
  )

  let NEW_MAZE = nest(
    async function () {
      let $name

      let MAKE = nest(
        async function ({ name }) {
          await push("/mazes", { name })
        }
      )

      let CANCEL = nest(
        async function () {}
      )

      draw(
        <div className="dialog">
          <section>
            <h1>New maze</h1>
            <form
              onSubmit={e => {
                e.preventDefault()
                MAKE({ name: $name.value })
              }}
            >
              <input
                ref={x => $name = x}
                placeholder="Name of maze..."
              />
              <span
                className="button"
                onClick={() => MAKE({ name: $name.value })}
              >
                Make new maze
              </span>
              <span
                className="button"
                onClick={CANCEL}
              >
                Cancel
              </span>
            </form>
          </section>
        </div>
      )

      await pick([MAKE, CANCEL])
    }
  )

  let Menu = ({
    name
  }) => (
    <div className="spot-menu" style={{ bottom: "10%", right: "10%" }}>
      <center>{name}</center>
    </div>
  )

  let Maze = ({
    id,
    name,
    slug,
    spots,
  }) => {

    let nodes = new vis.DataSet(spots.map(
      x => ({
        id: x.id,
        label: `*${x.name}*`,
        image: thumbnailUrl(x.clip),
      })
    ))

    let edges = new vis.DataSet(doors.map(
      ({ src_id, dst_id }) => ({ from: src_id, to: dst_id }))
    )

    let $graph

    // TODO: use ref for init/deinit
    wait(
      () => new vis.Network(
        $graph,
        { nodes, edges },
        {
          locale: "maze",
          locales: {
            maze: {
              edit: "Edit",
              del: "Delete",
              addEdge: "Add door",
              addNode: "Add spot",
              edgeDescription: "Click and drag...",
              addDescription: "Add your spot...",
              editNode: "Enter spot",
              editEdge: "Edit door",
              back: "Back",
            }
          },
          height: "600px",
          physics: { enabled: true },
          layout: { randomSeed: 0 },
          nodes: {
            font: {
              size: 16,
              face: "fantasque sans mono",
              multi: "md",
            },
            shape: "circularImage",
            mass: 5,
            size: 50,
          },
          edges: {
            arrows: { to: { enabled: true } }
          },
          manipulation: {
            enabled: true,
            addNode: false,
            initiallyActive: true,
            addEdge: (data, callback) => {
              ADD_EDGE({
                src: get(spots, data.from),
                dst: get(spots, data.to),
              })
            },
            addNode: (node, callback) =>
              ADD_NODE({ node, callback }),
            editNode: (node, callback) =>
              ENTER_SPOT({
                node,
                callback,
                maze: get(mazes, id),
              }),
          },
        }
      )
    )

    return (
      <section>
        <h1>{name}</h1>
        {
          <div ref={x => $graph = x}/>
        }
      </section>
    )
  }

  let Spot = ({
    name, clip
  }) => (
    <div>
      <b>{name}</b>
    </div>
  )

  let Clip = clip => (
    <div className="clip">
      {
        <img
           style={{ width: "8rem" }}
           src={thumbnailUrl(clip)}
        />
      }
    </div>
  )

  let $files

  // Render the overview
  draw(
    <div>
      {mazes.map(Maze)}
      <span
        className="button"
        onClick={NEW_MAZE}>
        New maze
      </span>
      <span
        className="button"
        onClick={() => PLAY_WITH_GRAMMAR({})}>
        Play with grammar
      </span>
      <section>
        <h1>Clips</h1>
        <div className="clips">
          {
            clips.length > 0
              ? clips.map(Clip)
              : <span>You have no clips yet.</span>
          }
        </div>
        <form>
          <label className="button">
            <span>
              Upload
            </span>
            <input
              ref={x => $files = x}
              type="file"
              multiple={true}
              onChange={() => UPLOAD_CLIPS($files.files)}
              style={{ display: "none" }}
            />
          </label>
          <span
            className="button"
            onClick={ () => REFRESH_CLIPS() }
          >
            Refresh
          </span>
        </form>
      </section>
      {
        false &&
          <div style={{ position: "relative" }}>
            <div ref={x => blender(x)}/>
            <span style={{
              position: "absolute", top: "50%", left: "40%", fontSize: 40, fontWeight: "bold", textShadow: "0 0 50px #ffffff"
            }}>
              LASTADIJA
            </span>
          </div>
      }
    </div>
  )

  // Wait for any of the nested actions
  await pick([
    NEW_MAZE,
    UPLOAD_CLIPS,
    REFRESH_CLIPS,
    ADD_EDGE,
    ADD_NODE,
    ENTER_SPOT,
    PLAY_WITH_GRAMMAR,
  ])

  // Show "Cool!" for a while
  draw(Splash("Cool!"))
  await doze(0.5)

  // Restart the overview
  await OVERVIEW({ mazes, clips, doors })
}

let Splash = x =>
  <div className="centered">
    {x}
  </div>

let Spin = x =>
  <div className="centered">
    <div className="spinner"></div>
    {x}
  </div>

let jpegs = xs => xs.filter(x => x.kind == "JPEG")

let videoSource = x =>
  `${cdn}/${x.name}/1080p-vp9/clip.webm`

let thumbnailUrl = x =>
  `${cdn}/${x.name}/480p-vp9/thumbnail-00002.jpg`

// Blender stuff

const T = THREE

let blender = root => {
  if (root == null)
    return

  let w = window.innerWidth
  let h = w * (9 / 16)

  var scene = new THREE.Scene();
  // var camera = new THREE.PerspectiveCamera( 75, w / h, 0.1, 1000 );

  var renderer = new THREE.WebGLRenderer({
    antialias: true,
    alpha: true,
  });
  renderer.setSize( w, h );
  root.appendChild( renderer.domElement );

  // var geometry = new THREE.BoxGeometry( 0.1, 0.1, 0.1 );
  // var material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );
  // var cube = new THREE.Mesh( geometry, material );
  // scene.add( cube );

  var light = new THREE.HemisphereLight( 0xffffbb, 0x080820, 1 );
  scene.add( light )

// scene.background = new T.Color(0xffffff)

//  camera.position.z = 5;

  let loader = new T.ObjectLoader()
  let geometryLoader = new T.JSONLoader()
  loader.load("last2.json", g => {
    scene.add(g)
    let clock = new T.Clock()
    let mixer = new T.AnimationMixer(scene)

    mixer.clipAction(g.animations[0])
      .setDuration(7)
      .startAt(0)
      .play()

    function loop () {
      mixer.update(clock.getDelta())
      renderer.render(scene, g.children[0])
      requestAnimationFrame(loop)
    }

    scene.fog = new T.FogExp2(0xffffff, 0.09)

    // let textureLoader = new T.TextureLoader()
    // let tf0 = textureLoader.load("lensflare0.png")
    // let tf1 = textureLoader.load("lensflare2.png")
    // let tf2 = textureLoader.load("lensflare3.png")
    // let pl = new T.PointLight(0xffffff, 1.5, 2000)
    // pl.position.set(0, 0, 20)
    // let lf = new T.LensFlare(tf0, 700, 0.0, T.AdditiveBlending, new T.Color(0xffffff))
    // lf.add(tf1, 512, 0.0, T.AdditiveBlending)
    // lf.add(tf1, 512, 0.0, T.AdditiveBlending)
    // lf.add(tf1, 512, 0.0, T.AdditiveBlending)
    // lf.add(tf2, 60, 0.6, T.AdditiveBlending)
    // lf.add(tf2, 70, 0.7, T.AdditiveBlending)
    // lf.add(tf2, 120, 0.9, T.AdditiveBlending)
    // lf.add(tf2, 70, 1.0, T.AdditiveBlending)
    // lf.position.copy(pl.position)
    // //lf.customUpdateCallback = lfUpdateCallback
    // scene.add(lf)

    geometryLoader.load("buildings.json", (buildings, materials) => {
      let mesh = new T.Mesh(buildings, materials)
      scene.add(mesh)
      loop()
    })
  })
}

var grammar = Maze
let MazeEng = Maze.concretes.MazeEng

let uniq = xs => [...new Set(xs)]

let tree = async xs => {
  let next = await pgf.completions(xs)
  let done = next.length == 0
  return { done, next }
}

let capitalize = s => s.charAt(0).toUpperCase() + s.slice(1)

let PLAY_WITH_GRAMMAR = nest(
  async function ({
    prefix = []
  }) {
    let { done, next } = await tree(prefix)
    let OK = nest(async () => {})
    let APPEND = nest(
      async function (x) {
        await PLAY_WITH_GRAMMAR({ prefix: [...prefix, x] })
      }
    )

    let CLEAR = nest(async () => await PLAY_WITH_GRAMMAR({}))

    draw(
      <div className="dialog">
        <section>
          <h1>Grammar Playground</h1>
          <div>"{capitalize(prefix.join(' '))}{done ? '.' : "..."}"</div>
          <ul className="words">
            {
              next.map(x =>
                <li
                  className="button"
                  onClick={() => APPEND(x)}>
                  {x}
                </li>
              )
            }
          </ul>
          <span className="button" onClick={CLEAR}>
            Reset sentence
          </span>
          <span className="button" onClick={OK}>
            Exit
          </span>
        </section>
      </div>
    )

    await pick([OK, APPEND, CLEAR])
  }
)