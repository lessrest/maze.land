// Settings

const apiHost =
  document.location.host.replace(/^www\./, "api.")
const apiRoot =
  `${document.location.protocol}//${apiHost}`

const putHost =
  document.location.host.replace(/^www\./, "put.")
const putRoot =
  `${document.location.protocol}//${putHost}`

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

let loop = Symbol("loop")
let yell = (...x) => console.info(...x)
let wait = (f, x) => setTimeout(f, x)
let plan = x => new Promise(x)
let meow = x => ReactDOM.render(x, document.querySelector("#app"))
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
  meow(Loading("Loading mazes..."))
  let mazes = await pull("/mazes")
  await doze(0.5)
  meow(Loading("Loading clips..."))
  let clips = await pull("/clips")
  meow(Loading("Loading doors..."))
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
            meow(Loading(`Uploading ${file.name}: ${(ratio * 100).toFixed(2)}...`))
          }
        }
        meow(Splash("Done!"))
        await doze(0.5)
      }
    }
  )

  let REFRESH_CLIPS = nest(
    async function () {
      meow(Loading("Refreshing clips..."))
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
          meow(Loading("Making new spot..."))
          await doze(0.5)
          await push(`/spots`, { maze, clip, name })
          meow(Loading("Refreshing mazes..."))
          mazes = await pull("/mazes")
        }
      )

      let CANCEL = nest(
        async function () {
          meow(Splash("Cancelled."))
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
            jpegs(clip.clipfiles).length
              ?
                <img
                 style={{ width: "8rem" }}
                 src={`${apiRoot}/blobs/${jpegs(clip.clipfiles)[0].sha2}`}
                />
              : `${name}`

          }
        </div>
      )

      let $maze

      meow(
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
                  clips.map(x =>
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
                  src={videoSource(clip.clipfiles)}
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
      meow(Loading("Saving edge..."))
      await push("/doors", { src: src.id, dst: dst.id })
      meow(Loading("Refreshing doors..."))
      doors = await pull("/doors")
    }
  )

  let ENTER_SPOT = nest(
    async function ({
      node, callback, maze
    }) {
      let spot = get(maze.spots, node.id)
      meow(
        <div className="game">
          <video
            autoPlay
            loop
            src={videoSource(spot.clip.clipfiles)}
          />
          { Menu({ name: spot.name }) }
        </div>
      )
      await doze(3)
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
              face: "source code pro",
              multi: "md",
            },
            shape: "circularImage",
            mass: 2.6,
            size: 60,
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
        jpegs(clip.clipfiles).length
          ?
            <img
             style={{ width: "8rem" }}
             src={`${apiRoot}/blobs/${jpegs(clip.clipfiles)[0].sha2}`}
            />
          : `${name}`

      }
    </div>
  )

  let $files

  // Render the overview
  meow(
    <div>
      {mazes.map(Maze)}
      <section>
        <h1>Clips</h1>
        <div className="clips">
          {clips.map(Clip)}
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
      <div style={{ position: "relative" }}>
        <div ref={x => blender(x)}/>
        <span style={{
          position: "absolute", top: "50%", left: "40%", fontSize: 40, fontWeight: "bold", textShadow: "0 0 50px #ffffff"
        }}>
          LASTADIJA
        </span>
      </div>
    </div>
  )

  // Wait for any of the nested actions
  await pick([
    UPLOAD_CLIPS,
    REFRESH_CLIPS,
    ADD_EDGE,
    ADD_NODE,
    ENTER_SPOT,
  ])

  // Show "Cool!" for a while
  meow(Splash("Cool!"))
  await doze(0.5)

  // Restart the overview
  await OVERVIEW({ mazes, clips, doors })
}

let Splash = x =>
  <div className="centered">
    {x}
  </div>

let Loading = x =>
  <div className="centered">
    <div className="spinner"></div>
    {x}
  </div>

let jpegs = xs => xs.filter(x => x.kind == "JPEG")

let videoSource = xs =>
  `${apiRoot}/blobs/${xs.filter(x => x.kind == "MP4 H264 Vorbis")[0].sha2}`

let thumbnailUrl = x =>
  `${apiRoot}/blobs/${jpegs(x.clipfiles)[0].sha2}`

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
