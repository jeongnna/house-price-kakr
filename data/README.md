## File descriptions

    - train.csv - 예측 모델을 만들기 위해 사용하는 학습 데이터입니다. 집의 정보와 예측할 변수인 가격(Price) 변수를 가지고 있습니다.
    - test.csv - 학습셋으로 만든 모델을 가지고 예측할 가격(Price) 변수를 제외한 집의 정보가 담긴 테스트 데이터 입니다.
    - sample_submission.csv - 제출시 사용할 수 있는 예시 submission.csv 파일입니다.

## Data fields

    1. ID : 집을 구분하는 번호
    2. date : 집을 구매한 날짜
    3. price : 집의 가격(Target variable)
    4. bedrooms : 침실의 수
    5. bathrooms : 화장실의 수
    6. sqft_living : 주거 공간의 평방 피트(면적)
    7. sqft_lot : 부지의 평방 피트(면적)
    8. floors : 집의 층 수
    9. waterfront : 집의 전방에 강이 흐르는지 유무 (a.k.a. 리버뷰)
    10. view : 집이 얼마나 좋아 보이는지의 정도
    11. condition : 집의 전반적인 상태
    12. grade : King County grading 시스템 기준으로 매긴 집의 등급
    13. sqft_above : 지하실을 제외한 평방 피트(면적)
    14. sqft_basement : 지하실의 평방 피트(면적)
    15. yr_built : 지어진 년도
    16. yr_renovated : 집을 재건축한 년도
    17. zipcode : 우편번호
    18. lat : 위도
    19. long : 경도
    20. sqft_living15 : 2015년 기준 주거 공간의 평방 피트(면적, 집을 재건축했다면, 변화가 있을 수 있음)
    21. sqft_lot15 : 2015년 기준 부지의 평방 피트(면적, 집을 재건축했다면, 변화가 있을 수 있음)
