import {React, useState, useEffect} from 'react';
import {ConfigProvider, Layout, theme} from 'antd';
import {Divider} from 'antd';
import QuickAccess from './QuickAccess'
import ToolBar from './ToolBar'
import PaperList from './PaperList'
import SearchBar from './SearchBar'
import Paper from './Paper';
import {Content} from 'antd/es/layout/layout';
import './App.css'

const {Header, Sider} = Layout;

const onresize = () => {
    window.onresize = fixSearchbar;
    setTimeout(fixSearchbar, 10)
    setTimeout(fixSearchbar, 100)
    setTimeout(fixSearchbar, 1000)
};

const fixSearchbar = () => {
    document.getElementById("searchbar").parentElement.style.width =
        document.getElementById("header").clientWidth -
        document.getElementById("toolbar").clientWidth - 10 + 'px';
}

const App = () => {
    const {token: {colorBgContainer}, } = theme.useToken();
    const [favorite, setFavorite] = useState([]);
    const [recent, setRecent] = useState([]);
    const [tags, setTags] = useState([]);
    const [paper, setPaper] = useState({"paper":[], "tags":[]})
    const [paperId, setPaperId] = useState(1)
    useEffect(fixSearchbar);
    useEffect(onresize);
    useEffect(() => {
        fetch("/api/favorites")
            .then((response) => response.json())
            .then((f) => {setFavorite(f)})
    }, [])
    useEffect(() => {
        fetch("/api/tags")
            .then((response) => response.json())
            .then((t) => {setTags(t)})
    }, [])
    useEffect(() => {
        fetch("/api/get?id=" + paperId)
            .then((response) => response.json())
            .then((p) => {setPaper(p)})
    }, [paperId])

    return (
        <ConfigProvider theme={{
            algorithm: [theme.defaultAlgorithm, theme.compactAlgorithm],
            components: {
                Menu: {
                    subMenuItemBg: 'transparent',
                    itemHoverBg: 'transparent',
                    itemActiveBg: 'rgba(0, 0, 0, 0.10)',
                    itemSelectedBg: 'rgba(0, 0, 0, 0.10)',
                    itemSelectedColor: 'rgba(0, 0, 0, 0.88)',
                },
            },
        }}>
            <Layout
                style={{height: '100vh', overflow: 'hidden', backgroundColor: 'transparent'}}
            >
                <Sider width={'15em'} style={{backgroundColor: 'transparent', paddingTop: '40px'}}>
                    <QuickAccess tags={tags} favorite={favorite} recent={recent}/>
                </Sider>
                <Layout style={{background: colorBgContainer}}>
                    <Header id="header" style={{background: colorBgContainer, padding: '0em'}}>
                        <ToolBar setPaperId={setPaperId} />
                        <SearchBar />
                    </Header>
                    <Divider type='horizontal' style={{display: 'inline-flex', margin: '0em'}} />
                    <Content style={{background: colorBgContainer}}>
                        <Layout style={{background: colorBgContainer, height: '100%'}}>
                            <Sider width={'24em'} style={{background: colorBgContainer}}>
                                <PaperList />
                            </Sider>
                            <Content style={{background: colorBgContainer}}>
                                <Layout style={{background: colorBgContainer, height: '100%'}}>
                                    <Sider width={'1em'} style={{background: colorBgContainer, height: '100%'}}>
                                        <Divider type='vertical' style={{height: '100%'}} />
                                    </Sider>
                                    <Content style={{background: colorBgContainer}}>
                                        <Paper paper={paper} />
                                    </Content>
                                </Layout>
                            </Content>
                        </Layout>
                    </Content>
                </Layout>
            </Layout>
        </ConfigProvider>
    );
};
export default App;
