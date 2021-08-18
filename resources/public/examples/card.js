let md = markdownit();
document.querySelectorAll(".about").forEach((e)=>{
    e.innerHTML = md.render(e.innerHTML.replaceAll(/\ +/ig, " "));
});

document.querySelectorAll("h1").forEach((e) => {
    var text = e.innerText;
    e.innerHTML = `<a id="${text}" href="#${text}">${text}</a>`;
});
