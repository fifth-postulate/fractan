(function(){
  let slideshow = remark.create({
    sourceUrl: 'presentation.md'
  });
  slideshow.on('showSlide', once(function(){
    turn('fractan').into(function(container, source){
          let description = JSON.parse(source);
          let flags = { description: description };
          let app = Elm.Fractan.init({
            node: container,
            flags: flags
          });
    });
  }));

  function once(callback){
    var called = false;
    return function(){
      if (!called) {
        called = true;
        callback();
      }
    }
  }

  function turn(className){
    return {
      into: function(callback){
        let samples = Array.prototype.slice.call(document.getElementsByTagName('div'))
          .filter(function(sample){ return sample.classList.contains(className)});
        samples.forEach(function(sample){
          let source = sample.getElementsByTagName('p')[0].innerText;
          callback(sample, source);
       });
      }
    }
  }

 
  // Setup MathJax
  MathJax.Hub.Config({
    tex2jax: {
      skipTags: ['script', 'noscript', 'style', 'textarea', 'pre']
    }
  });
  MathJax.Hub.Configured();
})();
