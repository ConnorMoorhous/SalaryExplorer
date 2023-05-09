let width = 0
let height = 0

addEventListener("load", (event) => {})
addEventListener("resize", (event) => {})

onload = (event) => {
    updateWindowDimensions()
    resizeContent()
}

onresize = (event) => {
    updateWindowDimensions()
    resizeContent()
}

function updateWindowDimensions() {
    width = window.innerWidth
    height = window.innerHeight
}

function resizeContent() {
    let headerHeight = document.getElementById("header").offsetHeight
    document.getElementById("content").style.minHeight = `${height - headerHeight}px`
}

Shiny.addCustomMessageHandler("controlLabel", updateLabel)
        
function updateLabel(message) {
    document.querySelector(`button[data-id="${message.id}"]`)
    .querySelector('.filter-option-inner-inner')
    .innerHTML = message.label
}