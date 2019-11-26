import { Elm } from './elm/Main.elm'
import { promisify } from 'elm-promisify'

const flags = {}
const node = document.querySelector('[data-elm-entry]')

promisify(Elm.Main, { node, flags })
  .then(app => {

  })
