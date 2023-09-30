const {v3} =require("../../murmurhash/murmurhash.js");
const formatSecs = x=>new Date(x).toISOString().slice(0,19);
const showSecs   = x=>formatSecs(x).replace("T"," ");
export{v3,formatSecs,showSecs}