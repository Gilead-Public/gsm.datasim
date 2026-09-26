HTMLWidgets.widget({
    name: 'Widget_TestDataExplorer',
    type: 'output',
    factory: function (el, width, height) {
        let instance = null;
        return {
            renderValue: function (x) {
                el.innerHTML = '';
                instance = gsmTestdataExplorer.render(el, x);
            },
            resize: function (width, height) {
                if (instance && typeof instance.resize === 'function') {
                    instance.resize(width, height);
                }
            },
        };
    },
});
