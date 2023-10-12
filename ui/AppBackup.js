import {React, useState} from 'react';
import {SearchOutlined} from '@ant-design/icons';
import {ConfigProvider, Layout, Menu, theme} from 'antd';
import {Tag, Divider, Input, Select} from 'antd';

const {Header, Sider} = Layout;
const {token: {colorBgContainer}, } = theme.useToken();

function getItem(label, key, icon, children, type) {
    return {key, icon, children, label, type};
}

const tagRender = (props) => {
    const {label, value, closable, onClose} = props;
    const onPreventMouseDown = (event) => {
        event.preventDefault();
        event.stopPropagation();
    };
    return (
        <Tag
            onMouseDown={onPreventMouseDown}
            closable={closable}
            onClose={onClose}
            style={{marginRight: 3, fontWeight: 'bold'}}
        >
            {'#' + label}
        </Tag>
    );
};

const items = [
    getItem('Favorite', 'favourite', null, [getItem('Option 13', '0'), getItem('Option 14', '1')], 'group'),
    getItem('Recent', 'recent', null, [getItem('Option 13', '3'), getItem('Option 14', '4')], 'group'),
    getItem('Tags', 'tags', null, [getItem('Option 13', '5'), getItem('Option 14', '6')], 'group'),
];

const OPTIONS = ['Apples', 'Nails', 'Bananas', 'Helicopters'];

const SearchBar = () => {
    const [selectedItems, setSelectedItems] = useState([]);
    const filteredOptions = OPTIONS.filter((o) => !selectedItems.includes(o));
    return (
        <div style={{textAlign: 'right'}}>
            <Select
                showSearch
                allowClear
                tagRender={tagRender}
                mode="multiple"
                suffixIcon={null}
                autoFocus={true}
                placeholder="Filter Tags"
                style={{minWidth: '20%', marginRight: '1em', textAlign: 'left'}}
                value={selectedItems}
                onChange={setSelectedItems}
                options={filteredOptions.map((item) => ({value: item, label: item}))}
            />
            <Input
                allowClear
                placeholder="Search"
                style={{width: '30%'}}
                prefix={<SearchOutlined className="site-form-item-icon" />}
            />
        </div>);
};

const ToolBar = () => {
    return (
        <div>
        </div>
    )
}


const App = () => {
    return (
        <ConfigProvider theme={{algorithm: [theme.defaultAlgorithm, theme.compactAlgorithm]}}>
            <Layout
                style={{minHeight: '100vh', }}
            >
                <Sider style={{background: colorBgContainer}}>
                    <Menu items={items} style={{padding: '1em', minHeight: '100vh'}} />
                </Sider>
                <Layout>
                    <Header style={{background: colorBgContainer, width: '100%'}}>
                        <ToolBar />
                        <Divider type='vertical' />
                        <SearchBar />
                    </Header>
                </Layout>
            </Layout>
        </ConfigProvider>
    );
};
export default App;
