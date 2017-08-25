function fetchBase64(url, callback) {
  console.log(`JavaScript: Fetching ${url}...`)
  fetch(url)
    .then(function (x) { return x.blob() })
    .then(function (x) {
      var r = new FileReader()
      r.onload = function () {
        console.log(`JavaScript: Invoking Haskell callback with Base64 result...`)
        callback(r.result.substr(r.result.indexOf(",") + 1))
      }
      r.readAsDataURL(x)
    })
}

function minimap(xs) {
  console.log(xs)
  setTimeout(function () {
    startMinimap(xs)
  }, 2000)
}