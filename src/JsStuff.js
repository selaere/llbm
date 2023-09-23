const {v3} =require("../../murmurhash/murmurhash.js");
const formatSecs = x=>new Date(x).toISOString();
const showSecs   = x=>formatSecs(x).slice(0,19).replace("T"," ");
export{v3,formatSecs,showSecs}