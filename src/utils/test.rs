#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct TestCaseInfo {
    pub name: String,
    pub compileexitcode: i32,
}
