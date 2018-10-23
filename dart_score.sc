/**
 * @param result 다트 던진 결과
 * @return 환산 점수
 */
def dartScore(result: String) = {
  // 점수만 추출
  val number = result.split("[S,D,T]{1}[*,#]?").map(_.toDouble)

  def _dartScore(str: String, order: Int = 0, index: Int = 0): Int = {
    if (str.length > order) {
      str(order) match {
        case 'T' => number(index) = Math.pow(number(index), 3)
        case 'D' => number(index) = Math.pow(number(index), 2)
        case '*' if index > 0 =>
          number(index - 1) = number(index - 1) * 2
          number(index) = number(index) * 2
        case '*' => number(index) = number(index) * 2
        case '#' => number(index) = number(index) * (-1)
        case _ => number(index)
      }

      // 보너스 점수(SDT)일때 인덱스 증가
      if (str.length > order + 1) {
        val char = str(order + 1)
        if (char == 'S' || char == 'D' || char == 'T') {
          _dartScore(str, order + 1, index + 1)
        } else _dartScore(str, order + 1, index)
      } else _dartScore(str, order + 1, index)
    } else {
      number.sum.toInt
    }
  }
  // 보너스 점수, 옵션 점수 기준으로 점수 계산
  _dartScore(result.replaceAll("\\d", ""))
}

dartScore("1S2D*3T")
dartScore("1D2S#10S")
dartScore("1D2S0T")
dartScore("1S*2T*3S")
dartScore("1D#2S*3S")
dartScore("1T2D3D#")
dartScore("1D2S3T*")