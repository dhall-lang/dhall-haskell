require('js-yaml')

function jsonToYaml(json) {
    let obj = JSON.parse(json);
    let str = jsyaml.safeDump(obj);
    return str;
}
