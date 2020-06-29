const DARK_MODE_OPT = 'dark-mode'
const DARK_MODE_ACTIVE = 'dark-mode-active'
const DARK_MODE_INACTIVE = 'dark-mode-inactive'

function onReady() {
  if (localStorage.getItem(DARK_MODE_OPT) == DARK_MODE_ACTIVE) {
    document.body.classList.add('dark-mode')
  } else {
    document.body.classList.remove('dark-mode')
  }
  document.getElementById('switch-light-dark-mode')
    .addEventListener('click', () => {
      document.body.classList.toggle('dark-mode')
      if (document.body.classList.contains('dark-mode')) {
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
}

if (document.readyState != 'loading'){
  onReady();
} else {
  document.addEventListener('DOMContentLoaded', onReady);
}
