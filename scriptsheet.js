    function myFunction() {
    var x = document.getElementById("mynav");
    if (x.className === "navigation") {
        x.className += " responsive";
    } else {
        x.className = "navigation";
    }
}
function fadeIn(el, time) {
    el.style.display = "block";
    el.style.opacity = 0;

    var last = +new Date();
    var tick = function() {
        el.style.opacity = +el.style.opacity + (new Date() - last) / time;
        last = +new Date();

    if (+el.style.opacity < 1) {
      (window.requestAnimationFrame && requestAnimationFrame(tick)) || setTimeout(tick, 16);
    }
  };

  tick();
}

function toggle_show(number) {
    var name_1 = "abstract_".concat(number);
    var name_2 = "abstract_hide_btn_".concat(number);
    var name_3 = "abstract_show_btn_".concat(number);

    var abstract = document.getElementById(name_1);
    var hide_btn = document.getElementById(name_2);
    var show_btn = document.getElementById(name_3);

    if (abstract.style.display === "none") {
        fadeIn(abstract,2000);
        hide_btn.style.display = "block";
        show_btn.style.display = "none";
    } else {
        abstract.style.display = "none";
        hide_btn.style.display = "none";
        show_btn.style.display = "block";
    }
}


document.addEventListener('DOMContentLoaded', function() {
    document.getElementById("abstract_1").style.display = "none";
    document.getElementById("abstract_show_btn_1").style.display = "block";
    document.getElementById("abstract_2").style.display = "none";
    document.getElementById("abstract_show_btn_2").style.display = "block";
    document.getElementById("abstract_3").style.display = "none";
    document.getElementById("abstract_show_btn_3").style.display = "block";
    document.getElementById("abstract_4").style.display = "none";
    document.getElementById("abstract_show_btn_4").style.display = "block";
}, false);