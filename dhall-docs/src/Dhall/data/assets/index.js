const DARK_MODE_OPT = 'dark-mode'
const DARK_MODE_ACTIVE = 'dark-mode-active'
const DARK_MODE_INACTIVE = 'dark-mode-inactive'

if (!localStorage.hasOwnProperty(DARK_MODE_OPT) && window.matchMedia('(prefers-color-scheme: dark)').matches
    || localStorage.getItem(DARK_MODE_OPT) == DARK_MODE_ACTIVE) {
  document.documentElement.classList.add('dark-mode')
}

function onReady() {
  document.getElementById('switch-light-dark-mode')
    .addEventListener('click', () => {
      document.documentElement.classList.toggle('dark-mode')
      if (document.documentElement.classList.contains('dark-mode')) {
        localStorage.setItem(DARK_MODE_OPT, DARK_MODE_ACTIVE)
      } else {
        localStorage.setItem(DARK_MODE_OPT, DARK_MODE_INACTIVE)
      }
    })

  document.querySelectorAll('a.copy-to-clipboard').forEach(node =>
    node.addEventListener('click', () => {
      const temp = document.createElement('input')
      document.body.append(temp)
      temp.value = node.dataset.path
      temp.select()
      document.execCommand("copy");
      temp.remove()
    }))

  document.querySelectorAll('.source-code a.name-use, .source-code span.name-decl')
    .forEach(node => {
      node.addEventListener('mouseover', () => highlightNames(node.dataset.name, true))
      node.addEventListener('mouseout', () => highlightNames(node.dataset.name, false))
    })
}

function highlightNames(varId, highlight) {
  document.querySelectorAll(`#${varId}, [href="#${varId}"]`).forEach(node => {
    if (highlight) {
      node.classList.add('highlighted')
    } else {
      node.classList.remove('highlighted')
    }
  })
}


if (document.readyState != 'loading'){
  onReady();
} else {
  document.addEventListener('DOMContentLoaded', onReady);
}
