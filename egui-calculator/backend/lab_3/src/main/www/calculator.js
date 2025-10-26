function mapContentWith (data, k) {
  let name = '';
  switch (data.$type) {
    case 'GraphvizFigure': {
      name = data.title;
      const source = data.source;
      Viz.instance().then(viz => {
        const graph = viz.renderSVGElement(source);
        const html = graph.outerHTML;
        k({ name, html, multistep: false });
      });
      break;
    }
    case 'PlainText': {
      name = data.title;
      const text = data.text;
      const html = `<pre>${text}</pre>`;
      k({ name, html, multistep: false });
      break;
    }
    case 'ReductionSeq': {
      name = data.title;
      const steps0 = data.steps;
      Viz.instance().then(viz => {
        const steps1 = steps0.map(step => {
          const graph = viz.renderSVGElement(step[1]);
          const res = { expr: step[0], graph: graph.outerHTML };
          return res;
        });
        k({ name, steps: steps1, multistep: true });
      });
      break;
    }
    default: {
      console.log('unrecognized data: ', data);
      break;
    }
  }
}

const app = new Vue({
  el: '#app',
  data: {
    message: 'Hello Vue!',
    graph: '',
    isError: false,
    errorMessage: '',
    contents: []
  },
  methods: {
    onShowClick: function () {
      this.isError = false;
      this.contents = [];

      const expr = document.getElementById('expr').value;

      const exprtype = document.querySelector('input[name="exprtype"]:checked').value;

      fetch(`/calc?exprType=${exprtype}&expr=${encodeURIComponent(expr)}`)
        .then(response => response.json())
        .then(data => {
          if (data.Error) {
            this.isError = true;
            this.errorMessage = data.Error.msg;
          } else if (data.$type == 'Ok') {
            const xs = data.contents;
            xs.forEach(d => {
              const obj0 = { name: '...', html: '...', multistep: false };
              this.contents.push(obj0);
              mapContentWith(d, obj => {
                obj0.name = obj.name;
                obj0.multistep = obj.multistep;
                if (!obj.multistep) obj0.html = obj.html;
                else obj0.steps = obj.steps;
              });
            });
          }
        })
        .catch(err => {
          console.log(err);
          this.isError = true;
          this.errorMessage = err;
        });

      Viz.instance().then(viz => {
        const graph = viz.renderSVGElement('digraph { a -> b }');
        this.graph = graph.outerHTML;
      });
    }
  }
});
